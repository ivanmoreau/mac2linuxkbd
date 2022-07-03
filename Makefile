build:
	@echo "Building..."
	@ghc Main.hs -lX11 x11.c -fPIC -o mac2linuxkbd
	@make cleanArtifacts
	@echo "Done building."

cleanArtifacts:
	@echo "Cleaning artifacts..."
	@rm -f *.o *.hi *.dyn_*
	@echo "Done cleaning artifacts."

install:
	@echo "Installing..."
	@cp mac2linuxkbd /usr/local/bin/mac2linuxkbd
	@mkdir -p /etc/interception/
	@cp udevmon.yaml /etc/interception/udevmon.yaml
	@echo "Done."

enable:
	@echo "Enabling..."
	@systemctl enable udevmon.service
	@echo "Done."

disable:
	@echo "Disabling..."
	@systemctl disable udevmon.service
	@echo "Done."

start:
	@echo "Starting..."
	@systemctl start udevmon.service
	@echo "Done."

stop:
	@echo "Stopping..."
	@systemctl stop udevmon.service
	@echo "Done."

uninstall:
	@echo "Uninstalling..."
	@make stop
	@make disable
	@rm /usr/local/bin/mac2linuxkbd
	@rm /etc/interception/udevmon.yaml
	@echo "Done."

InterceptionToolsII:
	@echo "Interception tools."
	@echo "Installing dependencies..."
	@apt install cmake libevdev-dev libudev-dev libyaml-cpp-dev libboost-dev
	@echo "Done installing dependencies."
	@echo "Cloning interception tools..."
	@git clone https://gitlab.com/interception/linux/tools.git interception-tools
	@echo "Done cloning."
	@echo "Building interception tools..."
	@cd interception-tools && \
	 cmake -B build -DCMAKE_BUILD_TYPE=Release && \
	 make -C build -j$(nproc)
	@echo "Done building."
	@echo "Installing interception tools..."
	@cp interception-tools/build/intercept /usr/local/bin/intercept && \
	 cp interception-tools/build/udevmon /usr/local/bin/udevmon && \
	 cp interception-tools/build/mux /usr/local/bin/mux && \
	 cp interception-tools/build/uinput /usr/local/bin/uinput && \
	 ln -s /usr/local/bin/udevmon /usr/bin/udevmon
	@echo "Done installing."
	@echo "Installing Systemd service..."
	@cp interception-tools/udevmon.service /etc/systemd/system/udevmon.service
	@echo "Done installing."
	@echo "Cleaning up..."
	@rm -rf interception-tools
	@echo "Done."

InterceptionToolsUU:
	@echo "Uninstalling Interception Tools..."
	@rm -rf /usr/local/bin/intercept /usr/local/bin/udevmon /usr/local/bin/mux /usr/local/bin/uinput
	@echo "Done."
