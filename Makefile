# Image Describer.
#
#   make deps    restore the dependencies ocicl.csv pins, into ./ocicl/
#   make test    run the FiveAM suite
#   make run     run the application from source, unbundled
#   make app     build "build/Image Describer.app"
#   make icon    draw res/icon.png again, from tools/icon.lisp
#   make screenshot INPUT=folder   draw the window into doc/, describing FOLDER
#   make clean   remove the bundle and the fasls
#
# im is not in the ocicl registry, so it comes from a checkout: IM_DIR, by
# default the one beside this directory.  The IM shared libraries themselves
# come from Homebrew's tecgraf-im (brew install lispnik/tap/tecgraf-im), and
# the bundle carries copies of them.

SBCL ?= sbcl

# Signing, which is personal: local.mk, never committed.
-include local.mk

IM_DIR ?= ../im
# Sibling checkouts that shadow the pinned objc and asdf-macos-app.
OBJC_DIR ?=
# The bundle needs an asdf-macos-app that puts Contents/Frameworks on CFFI's
# search path before im's restore hook reopens its libraries; the release
# ocicl.csv pins does it later, and IM loads from Homebrew.  Until a release
# with that is pinned, the checkout beside this one; MACOS_APP_DIR= for the pin.
MACOS_APP_DIR ?= ../asdf-macos-app

# :IGNORE-INHERITED-CONFIGURATION so that a dependency missing from ocicl.csv
# fails here, rather than resolving to whatever the developer's own source
# registry happens to reach.  IM_DIR is a :DIRECTORY, not a :TREE: the checkout
# has an ocicl/ of its own, whose copies must not shadow the ones pinned here.
REGISTRY = (asdf:initialize-source-registry \
              (list :source-registry \
                    (list :directory (truename "$(IM_DIR)/")) \
                    $(if $(OBJC_DIR),(list :tree (truename "$(OBJC_DIR)/"))) \
                    $(if $(MACOS_APP_DIR),(list :tree (truename "$(MACOS_APP_DIR)/"))) \
                    (list :tree (truename "./")) \
                    :ignore-inherited-configuration))

LISP = $(SBCL) --non-interactive --no-userinit --no-sysinit \
         --eval '(require :asdf)' --eval '$(REGISTRY)'

.PHONY: deps test run app icon screenshot clean

deps:
	ocicl install

test:
	$(LISP) --eval '(asdf:load-system "image-describe/tests")' \
	        --eval '(uiop:quit (if (image-describe/tests:run-tests) 0 1))'

run:
	$(LISP) --eval '(asdf:load-system "image-describe")' \
	        --eval '(image-describe:main)'

app:
	$(LISP) --eval '(asdf:make "image-describe-app")'

icon:
	$(LISP) --eval '(asdf:load-system "im")' \
	        --load tools/icon.lisp \
	        --eval '(image-describe-icon:main)'

screenshot:
	INPUT="$(INPUT)" $(LISP) --eval '(asdf:load-system "image-describe")' \
	        --load tools/screenshot.lisp \
	        --eval '(image-describe-screenshot:main)'

clean:
	rm -rf build
	find . -name '*.fasl' -not -path './ocicl/*' -delete
