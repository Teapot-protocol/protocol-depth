# Depth

A sophisticated COBOL project demonstrating advanced programming concepts and complex data processing capabilities.

## Features

- Multi-dimensional array processing
- Complex mathematical computations
- File handling and data transformation
- Dynamic memory management
- Advanced string manipulation

## Structure

- `src/` - Contains the source code files
  - `main.cbl` - Main program file
  - `modules/` - Subprograms and modules
    - `matrix.cbl` - Matrix operations module
    - `compute.cbl` - Mathematical computations
    - `fileio.cbl` - File handling operations

## Building

Requires GnuCOBOL compiler (OpenCOBOL).

```bash
# Compile the main program
cobc -x -o depth src/main.cbl

# Run the program
./depth
```

## License

MIT License