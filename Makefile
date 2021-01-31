clean: 
	rm -f .envrc

.envrc: default.nix ## Rebuild nix packages and .envrc
	rm -f .envrc
	echo "PATH_add $$(nix-shell -j auto --cores 0 --pure --run 'printf %q $$PATH')" > .envrc
	direnv allow
