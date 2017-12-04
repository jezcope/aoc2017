SRC_HS := $(wildcard *.hs)
SRC_GO := $(wildcard *.go)
SRC_ALL := $(sort $(SRC_HS) $(SRC_GO))

OUT_ALL := $(basename $(SRC_ALL))

all: $(OUT_ALL)

clean:
	rm -f $(OUT_ALL)
	rm -f *.o *.hi

%: %.hs
	stack ghc $<

%: %.go
	go build $<

.PHONY: clean
