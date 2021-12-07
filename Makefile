nums = 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
numH = 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
numC = 06c

extra = s

mkday_fn = $(foreach i,$(1),day$(i))
mkall_fn = $(foreach i,$(1),$(i) $(i)$(extra))
mkexe_fn = $(foreach i,$(1),exe/$(i))

days = $(call mkday_fn,$(nums))
days_all = $(call mkall_fn,$(days))
days_extra = $(foreach i,$(days),$(i)$(extra))


# Haskell
daysH = $(call mkday_fn,$(numH))
days_allH = $(call mkall_fn,$(daysH))
days_exe = $(call mkexe_fn,$(days_allH))
days_hs = $(foreach i,$(days_allH),$(i).hs)

# C
daysC = $(call mkday_fn,$(numC))
days_allC = $(call mkall_fn,$(daysC))
days_c_extra = $(foreach i,$(daysC),$(i)$(extra))
days_c_exe = $(call mkexe_fn,$(days_allC))

all: $(days_all)
allh: $(days_allH)
allc: $(days_allC)

$(days_hs): %.hs:
	cat default.hs > $@

$(days_exe): exe/%: %.hs Utils.hs Parser.hs
	@echo === compiling to $@ ===
	ghc $*.hs -Wall -o $@

$(days): day%: exe/day% day%.in
	./exe/$@ < $@.in

$(days_extra): day%s: exe/day%s day%.in
	./exe/$@ < day$*.in

### C Only ##

$(days_c_exe): exe/%: %.c
	@echo === compiling to $@ '(from c)' ===
	gcc $*.c -Wall -Wextra -o $@

$(daysC): day%c: exe/day%c day%.in
	./exe/$@ < day$*.in

$(days_c_extra): day%cs: exe/day%cs day%.in
	./exe/$@ < day$*.in

clean:
	rm *.hi *.o exe/*
