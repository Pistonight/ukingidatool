use std::{
    collections::{BTreeMap, BTreeSet}, fs::File, io::{BufRead, BufReader, IsTerminal, Write}, path::Path, sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    }, thread::JoinHandle, time::Instant
};

use error_stack::{Report, Result, ResultExt};

#[inline]
pub fn read_as_bytes(file: impl AsRef<Path>) -> Result<Vec<u8>, std::io::Error> {
    Ok(std::fs::read(file)?)
}

#[derive(Debug, thiserror::Error)]
pub enum UkingParseError {
    #[error("Failed to open file")]
    OpenFile,
    #[error("Failed to read file")]
    ReadFile,
    #[error("Missing function address")]
    MissingFuncAddr,
    #[error("Invalid function address: {0}")]
    InvalidFuncAddr(String),
    #[error("Missing function status")]
    MissingStatus,
    #[error("Missing function size")]
    MissingSize,
    #[error("Missing data address")]
    MissingDataAddr,
}

pub fn read_uking_functions(file: impl AsRef<Path>, out: &mut BTreeMap<String, u64>) -> Result<(), UkingParseError> {
    let path = file.as_ref().display().to_string();
    let file = File::open(file).change_context(UkingParseError::OpenFile)
    .attach_printable_lazy(|| format!("Path: {}", path))?;
    let reader = BufReader::new(file);
    for line in reader.lines() {
        let line = line.change_context(UkingParseError::ReadFile)?;
        if let Some((addr, name)) = parse_uking_function(&line)? {
            out.insert(name.to_string(), addr);
        }
    }
    Ok(())
}

fn parse_uking_function(line: &str) -> Result<Option<(u64, &str)>, UkingParseError> {
    // examples:
    // 0x00000071000007a0,O,000032,_ZN4ksys3act8BaseProc11hasJobType_ENS0_7JobTypeE
    // 0x00000071000007c0,U,000156,ActorOption::m31
    if !line.starts_with("0x") {
        return Ok(None);
    }

    let mut parts = line.split(',');
    let addr = parts.next().ok_or(UkingParseError::MissingFuncAddr)?;
    let addr = u64::from_str_radix(&addr[2..], 16)
        .map_err(|_| UkingParseError::InvalidFuncAddr(addr.to_string()))?;

    let _ = parts.next().ok_or(UkingParseError::MissingStatus)?;
    let _ = parts.next().ok_or(UkingParseError::MissingSize)?;

    match parts.next() {
        Some(name) => Ok(Some((addr, name))),
        None => {
            // if the name is missing, we just ignore this line
            Ok(None)
        }
    }
}

pub fn read_uking_data(file: impl AsRef<Path>, out: &mut BTreeMap<String, u64>) -> Result<BTreeSet<String>, UkingParseError> {
    let path = file.as_ref().display().to_string();
    let file = File::open(file).change_context(UkingParseError::OpenFile)
    .attach_printable_lazy(|| format!("Path: {}", path))?;
    let reader = BufReader::new(file);
    let mut names = BTreeSet::new();
    for line in reader.lines() {
        let line = line.change_context(UkingParseError::ReadFile)?;
        if let Some((addr, name)) = parse_uking_data(&line)? {
            out.insert(name.to_string(), addr);
            names.insert(name.to_string());
        }
    }
    Ok(names)
}

fn parse_uking_data(line: &str) -> Result<Option<(u64, &str)>, UkingParseError> {
    // examples:
    // 0x0000007102606910,_ZN4ksys12SystemTimers9sInstanceE
    // 0x0000007102650608,_ZN4ksys3act11BaseProcMgr9sInstanceE
    if !line.starts_with("0x") {
        return Ok(None);
    }

    let mut parts = line.split(',');
    let addr = parts.next().ok_or(UkingParseError::MissingDataAddr)?;
    let addr = u64::from_str_radix(&addr[2..], 16)
        .map_err(|_| UkingParseError::InvalidFuncAddr(addr.to_string()))?;

    match parts.next() {
        Some(name) => Ok(Some((addr, name))),
        None => {
            // if the name is missing, we just ignore this line
            Ok(None)
        }
    }
}

pub struct ProgressPrinter {
    term_width: usize,
    total: usize,
    prefix: String,
    thread: Option<JoinHandle<()>>,
    can_print: Arc<AtomicBool>,
    is_done: Arc<AtomicBool>,
    start_time: Instant,
}

impl ProgressPrinter {
    pub fn new(total: usize, prefix: impl Into<String>) -> Self {
        let term_width = if std::io::stderr().is_terminal() {
            match terminal_size::terminal_size() {
                Some((width, _)) => width.0 as usize,
                None => 0,
            }
        } else {
            0
        };

        let prefix = prefix.into();
        let can_print = Arc::new(AtomicBool::new(true));
        let is_done = Arc::new(AtomicBool::new(false));

        // use a thread to throttle the printing
        let thread = {
            let can_print = Arc::clone(&can_print);
            let is_done = Arc::clone(&is_done);
            std::thread::spawn(move || {
                while !is_done.load(Ordering::Relaxed) {
                    std::thread::sleep(std::time::Duration::from_millis(50));
                    can_print.store(true, Ordering::Relaxed);
                }
            })
        };

        Self {
            term_width,
            total,
            prefix,
            thread: Some(thread),
            can_print,
            is_done,
            start_time: Instant::now(),
        }
    }

    pub fn print(&self, current: usize, text: impl std::fmt::Display) {
        if !self.can_print.load(Ordering::Relaxed) {
            return;
        }
        self.can_print.store(false, Ordering::Relaxed);
        let prefix = if self.total == 0 {
            format!("{1} {0} ", self.prefix, current)
        } else {
            let mut s = format!("[{1}/{2}] {0}: ", self.prefix, current, self.total);
            let elapsed = self.start_time.elapsed().as_secs_f32();
            if elapsed > 2.0 {
                let percentage = format!("{:.02}% ", (current as f32 / self.total as f32) * 100.0);
                let speed = current as f32 / elapsed;
                let eta = format!("ETA {:.02}s ", (self.total - current) as f32 / speed);
                s.push_str(&percentage);
                s.push_str(&eta);
            }
            s

        };
        let prefix_len = prefix.len();
        if prefix_len > self.term_width {
            eprintln!("{}{}", prefix, text);
            return;
        }
        let remaining = self.term_width - prefix_len - 1;
        let text = text.to_string();
        let text_len = text.len();
        let text = if text_len > remaining {
            let start = text_len - remaining;
            &text[start..]
        } else {
            &text
        };
        eprint!("\u{1b}[1K\r{}{}", prefix, text);
        let _ = std::io::stderr().flush();
    }

    pub fn set_total(&mut self, total: usize) {
        self.total = total;
    }

    pub fn reset_timer(&mut self) {
        self.start_time = Instant::now();
    }

    pub fn set_prefix(&mut self, prefix: impl Into<String>) {
        self.prefix = prefix.into();
    }

    pub fn done(&self) {
        self.is_done.store(true, Ordering::Relaxed);
        if self.total == 0 {
            println!("\u{1b}[1K\r{}", self.prefix);
        } else {
            println!("\u{1b}[1K\r[{1}/{1}] {0}", self.prefix, self.total);
        }
    }
}

impl Drop for ProgressPrinter {
    fn drop(&mut self) {
        self.is_done.store(true, Ordering::Relaxed);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}
