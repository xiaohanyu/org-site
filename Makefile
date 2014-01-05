EMACS ?= emacs
CASK ?= cask
ECUKES ?= ecukes
ERT_RUNNER ?= ert-runner
#$(shell find elpa/ecukes-*/ecukes | tail -1)

test: unit-tests

unit-tests: elpa
	${CASK} exec ${EMACS} -Q -batch -L . -L test \
		-l test/org-site-test.el -f ert-run-tests-batch-and-exit

ecukes-features: elpa
	${CASK} exec ${ECUKES} --no-win

elpa:
	mkdir -p .elpa
	${CASK} install 2> .elpa/install.log

clean-elpa:
	rm -rf .elpa

clean-elc:
	rm -f *.elc test/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}
	@echo ECUKES=${ECUKES}
	@echo ERT_RUNNER=${ERT_RUNNER}

travis-ci: print-deps test
