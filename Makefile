nums = 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
extra = s
days = $(foreach i,$(nums),day$(i) day$(i)$(extra))
days_exe = $(foreach i,$(days),exe/$(i))

$(days_exe): exe/%: %.hs
	@echo === compiling to $@ ===
	ghc $*.hs -o $@

$(days): day%: exe/day%
	./exe/$@ < $(subst $(extra),,$@).in

clean:
	rm *.hi *.o exe/*
