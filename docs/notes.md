# Memory Dispatch

Currently, I'm even more concerned with getting memory access and
memory mappers right than finishing the I/O, PPU, or APU.

A common trick I've seen in C emulators was to take some of the high
bits to figure out which 8k block of the address space was being
addressed, then dispatch off that value, i.e.

```
(let ((8k-block (ldb (byte 3 13) address)))
  (case 8k-block
    (0 (get-cpu address))
    (1 (get-ppu address))
    (2 (get-misc address)) ;; Sound and I/O
    (3 (get-save address)) ;; SRAM
    (t (get-mapper address))))
```

For some reason I expected this trick to be useful but a naive
**cond** was about 5 times faster in a simple benchmark. The
benchmarking could be flawed and maybe type annotations or something
could make the **case** faster but that's good enough for now.

# Memory Mapper Subsystem

The real question left is how to model the PPU, cartridge and it's
memory.  It may be an interesting opportunity to use Common Lisp's
[displaced arrays][disp_arr].  I expect I'll model the PPU's VRAM at
the very least as a displaced array into the ROM object.

The Memory Mapper Protocol has yet to be determined. A comparison
follows...

* **sprocketnes** API is: load-prg, load-chr, store-prg, store-chr, next-scanline
* **neth** API is: sync, init, write (using displaced arrays
                               to put data into write place for reading?)
* **tenes** API is: init, read, write, save-state, restore-state,
                    some next-scanline-ish stuff

MMC1 and MMC3 are somewhat complicated. The other 3 main mappers are
quite simple.  Let's implement them first.

tenes is the only "complete" implementation. neth sticks to mmc1/cnrom
and sprocketnes sticks to mmc1/mmc3.

For some reason, sprocketnes models load-chr and store-chr and makes
them part of a mapper's explicit interface. However, store-chr is
never used in sprocketnes and load-chr is only used during certain
VRAM accesses. tenes and neth only reference CHR in memcpy operations,
basically.  That makes sense to me as you should only want to block
copy sprites into the PPU. Maybe pcwalton is doing some kind of lazy
loading? At any rate, I should do some reading to ensure I maintain
correctness here but there is appeal to only needing init, read,
write, and next-scanline methods. I'll stick to that API if possible.

# Graphics Subsystem

I'd really like to take a cue from [sprocketnes][sprocketnes] and have
a static bytevector that I use as a buffer to draw in and occasionally
hand off to SDL to draw. I expect Stelian Ionescu's
[static-vectors][stat_vec] will be of help here but it may mean
digging around in the lispbuilder-sdl or cl-sdl2 internals to figure
out how to pass it. :-/

I'd also like to model the PPU as closely as possible off pcwalton's
version.  PPU has VRAM, OAM, and Registers, along with a screen and
some metadata slots.  VRAM itself has the mapper, nametables, and
palette. The mapper will be stored on the main NES struct in
famiclom. Mappers will likely be CLOS objects with a defined protocol.

[disp_arr]: http://lisptips.com/post/31516446212/using-an-adjustable-displaced-array-as-a-cursor-on
[stat_vec]: https://github.com/sionescu/static-vectors
[sprocketnes]: https://github.com/pcwalton/sprocketnes
