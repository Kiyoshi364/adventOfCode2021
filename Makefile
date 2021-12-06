nums = 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
numH = 01 02 03 04 05 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
extra = s
days = $(foreach i,$(nums),day$(i))
days_all = $(foreach i,$(days),$(i) $(i)$(extra))
days_extra = $(foreach i,$(days),$(i)$(extra))

daysH = $(foreach i,$(numsH),day$(i))
days_allH = $(foreach i,$(daysH),$(i) $(i)$(extra))

# Haskell
days_exe = $(foreach i,$(days_allH),exe/$(i))
days_hs = $(foreach i,$(days_allH),$(i).hs)

# C
special_c_exe = exe/day06 exe/day06s

all: $(days_all)

$(days_hs): %.hs:
	cat default.hs > $@

$(days_exe): exe/%: %.hs Utils.hs Parser.hs
	@echo === compiling to $@ ===
	ghc $*.hs -Wall -o $@

$(special_c_exe): exe/%: %.c
	@echo === compiling to $@ '(from c)' ===
	gcc $*.c -Wall -Wextra -o $@

$(days): day%: exe/day% day%.in
	./exe/$@ < $@.in

$(days_extra): day%s: exe/day%s day%.in
	./exe/$@ < day$*.in

clean:
	rm *.hi *.o exe/*
