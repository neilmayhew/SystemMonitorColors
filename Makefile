SHELL := nix-shell
.SHELLFLAGS := --pure --run

CPUS := $(shell nproc --all)
PROG := SystemMonitorColors.hs
RUNGHC := runghc -Wall -Wcompat

.PHONY: check set stress clean

check:
	$(RUNGHC) $(PROG) --unit

set:
	dconf write /org/gnome/gnome-system-monitor/cpu-colors "$$($(RUNGHC) $(PROG) -c $(CPUS) -d)"

stress:
	stress -c $(CPUS)

test-colors.html: $(PROG)
	$(RUNGHC) $< -c $(CPUS) --html >$@

clean:
	$(RM) test-colors.html
