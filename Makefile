SRC_HS := $(wildcard *.hs)
SRC_GO := $(wildcard *.go)
SRC_CC := $(wildcard *.cc)
SRC_ALL := $(sort $(SRC_HS) $(SRC_GO) $(SRC_CC))

OUT_ALL := $(basename $(SRC_ALL))

all: $(OUT_ALL)

clean:
	rm -f $(OUT_ALL)
	rm -f *.o *.hi

%: %.hs
	stack ghc $<

%: %.go
	go build $<

%: %.cc
	g++ -o $@ $<

.PHONY: clean
