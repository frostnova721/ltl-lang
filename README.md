# ltl-lang
Similar implementation of lox in C

Based on Lox from **Crafting Interpreters** by **Robert Nystrom**.

## Features
- Garbage collection
- Simple & Familiar syntax
- OOP Concept using classes
- Pretty fast!

## Building
- Clone the repository.
```bash
git clone https://github.com/frostnova721/ltl-lang
```

- Once that's done, just compile the entire thing with gcc!
``` bash
cd ltl-lang

gcc main.c chunk.c memory.c debug.c value.c vm.c compiler.c scanner.c object.c table.c -I . -o bin/ltl.exe
```

- Then just run it!
```bash
./bin/ltl.exe filename.ltl #for scripts
./bin/ltl.exe #for repl
```

**This project was purely for learning experience from the amazing book -- Crafing Interpreters.**
