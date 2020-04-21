SHELLS=zsh bash fish

test: test.zsh test.bash test.fish
	@printf "\e[34m\e[4m                                                                                \n\e[0m"

test.%: .tmp/%/_cmd .tmp/bin/cmd
	@printf "\e[34m\e[4m                                                                                \e[0m"
	@printf "\r\e[C\e[C\e[0;34m\e[4m$*\e[24m\n\e[0m" 
	@etc/capture $* 'cmd '
	@etc/capture $* 'cmd t'
	@etc/capture $* 'cmd th'
	@etc/capture $* 'cmd   something   t'

.tmp/bin/cmd: etc/cmd
	@mkdir -p .tmp/bin
	@cp etc/cmd .tmp/bin/cmd

.tmp/lib/exoskeleton-core.jar: $(wildcard src/core/*.scala)
	@fury build run --output linear --fat-jar --dir .tmp/lib

clean:
	@rm -rf .tmp

revise:
	@etc/revise

.SECONDARY:

.tmp/%/_cmd: .tmp/lib/exoskeleton-core.jar
	@mkdir -p .tmp/$*
	@java -cp ".tmp/lib/exoskeleton-core.jar" "exoskeleton.Generate" cmd "$*"

.PHONY: revise test clean
