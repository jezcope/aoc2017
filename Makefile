SRC_HS := $(wildcard [0-9][0-9]-*.hs)
SRC_GO := $(wildcard [0-9][0-9]-*.go)
SRC_CC := $(wildcard [0-9][0-9]-*.cc)
SRC_RS := $(wildcard [0-9][0-9]-*.rs)
SRC_ALL := $(sort $(SRC_HS) $(SRC_GO) $(SRC_CC) $(SRC_RS))

OUT_ALL := $(basename $(SRC_ALL))

all: $(OUT_ALL)

clean:
	rm -f $(OUT_ALL)
	rm -f *.o *.hi

%: %.hs
	stack ghc -- --make -rtsopts -prof $<

%: %.go
	go build $<

%: %.cc
	g++ -o $@ $<

%: %.rs
	rustup run stable rustc $<

.PHONY: clean
