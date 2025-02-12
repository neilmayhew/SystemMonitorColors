SHELL := nix-shell
.SHELLFLAGS := --pure --quiet --keep DBUS_SESSION_BUS_ADDRESS --run

CPUS := $(shell nproc --all)
TIMEOUT := 60
PROG := SystemMonitorColors.hs
RUNGHC := runghc -Wall -Wcompat

.PHONY: check set stress clean

check:
	$(RUNGHC) $(PROG) --unit

set:
	dconf write /org/gnome/gnome-system-monitor/cpu-colors "$$($(RUNGHC) $(PROG) -c $(CPUS) -d)"

stress:
	stress -c $(CPUS) -t $(TIMEOUT)

test-colors.html: $(PROG)
	$(RUNGHC) $< -c $(CPUS) --html >$@

clean:
	$(RM) test-colors.html
