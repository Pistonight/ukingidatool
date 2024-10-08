# UKing IDA Tool
Tool for importing the BOTW decompile project into IDA Pro.
All you need is the BOTW decompile project and no other dependencies.

What this tool can do:
- Import structures and enumeration types
- Set function return types and argument types
- Rename structure members and function arguments
  - Will not overwrite a temporary name with a stub name

This tool is only useful for using with the BOTW decompile project.
It cannot be used to play the game or any other game. This repo does not
contain any game files in any form.

## Requirements
- Setup the [BOTW Decompilation Project](https://botw.link)
  - You need to successfully produce a build by running the `ninja` command.
- IDA Pro 7.6+

## Installation
Build this tool from source via `cargo`. You should have it in WSL if your BOTW
decompile project is also in WSL.
```bash
cargo install ukingidatool --git https://github.com/Pistonight/ukingidatool
```
You should also have `python` installed so you can run IDAPython scripts.
If you setup the decompile project however, this should already be installed.
(No python dependencies are required on the Windows side.)

## Database from Scratch
To create the IDA database from scratch, first load `<botw>/data/main.elf`
into IDA (`<botw>` is the path to the BOTW decompile project).
You probably also want to rebase the program to `0x7100000000` to match the
decompile project:

 1. Edit > Segments > Rebase Program...
 2. Select "Image base" and enter `0x7100000000`
 3. Check both "Fix up the program" and "Rebase the whole image"
 4. Press OK

Now, wait for the initial auto analysis to finish, then save and close the database.

## Using the Tool

### IMPORTANT: ALWAYS ALWAYS backup the database before running ANY script generated by this tool. You have been warned.

The tool is designed to be used with the BOTW decompile project,
and will automatically find relavant information if you run inside the decompile project.

There are 4 steps to using the tool:
1. Build the decompile project to obtain DWARF information.
2. Run the `extract` command to parse the DWARF information.
3. Run the `import` command to generate an IDA script.
4. Run the generated IDA script in IDA.

The raw DWARF information is disorganized and not very useful.
The extraction step parses the DWARF and run extensive analysis to
extract information that will be useful when imported into IDA.
This step is slow, which is why the output is saved to a file
and can be reused when generating the import script.

### Extraction
Assuming you are in the decompile project and your build output is at `<botw>/build/uking`,
all you have to do is
```bash
ukingidatool extract
```
This will generate `<botw>/build/uking-extract.yaml`. You can manually inspect
the file if you want to see what information is extracted.

You can also specify the paths manually. See `ukingidatool extract --help` for more information.

### Generating Import Script

### IMPORTANT: ALWAYS ALWAYS backup the database before running ANY script generated by this tool. You have been warned.

If you want to import everything:
```bash
ukingidatool import
```
This will generate `<botw>/build/uking-import.py`.

You can fine-tune what to import with command line options. 
See `ukingidatool import --help` for all options. Some examples:

- Import with a different base address (default is `0x71`)
  ```bash
  ukingidatool import --address 0x00
  ```
- Import only types and functions containing a substring. Note that mangled names are used for decompiled functions.
  Dependent types are still imported recursively
  ```bash
  ukingidatool import --pattern "PauseMenuDataMgr"
  ```
- Only import types, don't rename functions or their arguments
  ```bash
  ukingidatool import --type-only
  ```
- Only rename functions, don't do anything to arguments or types.
  ```bash
  ukingidatool import --name-only
  ```
- Assume types are already imported - will error if they are not.
  This can be useful because importing types take a while
  ```bash
  ukingidatool import --skip-types
  ```

## Running the Import Script

First, make sure you backed up the database.

Then, run the generated script in IDA with File > Script File... and select the generated script.

After running the script, wait for the auto analysis to finish (should say `AU: idle` in the bottom-left), then try saving and reopen the database to make sure it's not corrupted.
I recommend still keeping the backup around just in case.

sometimes, renaming function argument types after importing types immediately will fail. You can rerun with `--skip-types` to retry.


