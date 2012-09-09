(in-package :famiclom)

;; Mappers 0-4 cover ~74% of all existing NES titles.
;; Support them fully. NROM, MMC1, UNROM, CNROM, MMC3
;; Or as tenes refers: none, mmc1, konami, vrom, mmc3

;; How do we handle unsupported mappers? mapper-supported-p?
;; How...the F...do we handle the standard NES memory map?
;; If we redefine the 6502-cpu functions for get/set-{byte,word,range},
;; the macros will pick it up at least.

(defmacro defmapper (name schema)
  "Generate a mapper class and read/init/shutdown methods from a schema."
  `(defclass ,name () ()))

;; Bigtime TENES methods: init, shutdown, read, write.
;; Also has scanline_start, scanline_end, save_state, restore_state, ex_write, ex_read.

(defmapper nrom '(:id 0))
; init: load the 8k chr block into the PPU's vram, clearing vram if no CHR is present.
; read: Index into PRG block at (addr & prg-size--)
; many other methods return constants. metadata block could hold them instead

(defmapper mmc1 '(:id 1))

(defmapper unrom '(:id 2))

(defmapper cnrom '(:id 3))

(defmapper mmc3 '(:id 4))
