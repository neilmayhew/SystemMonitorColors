SHELL := nix-shell
.SHELLFLAGS := --run

PROG := SystemMonitorColors.hs
RUNGHC := runghc -Wall -Wcompat

.PHONY: check set stress clean

check:
	$(RUNGHC) $(PROG) --unit

set:
	dconf write /org/gnome/gnome-system-monitor/cpu-colors "$$($(RUNGHC) $(PROG) -d)"

stress:
	stress -c 16

test-colors.html: $(PROG)
	$(RUNGHC) $< --html >$@

clean:
	$(RM) test-colors.html
