nums = 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
extra = s
days = $(foreach i,$(nums),day$(i))
days_extra = $(foreach i,$(days),$(i)$(extra))
days_exe = $(foreach i,$(days) $(days_extra),exe/$(i))
days_hs = $(foreach i,$(days) $(days_extra),$(i).hs)

$(days_hs): %.hs:
	echo -n 'main :: IO ()\nmain = putStrLn "Hello from $*:"\n      >>'\
		'getContents\n      >>= putStr . unlines . take 5 . lines\n\nsolve'\
		':: [Int] -> Int\nsolve = undefined' > $@

$(days_exe): exe/%: %.hs
	@echo === compiling to $@ ===
	ghc $*.hs -o $@

$(days): day%: exe/day% day%.in
	./exe/$@ < $@.in

$(days_extra): day%s: exe/day%s day%.in
	./exe/$@ < day$*.in

clean:
	rm *.hi *.o exe/*
