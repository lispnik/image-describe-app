# Image Describer.
#
#   make deps    restore the dependencies ocicl.csv pins, into ./ocicl/
#   make test    run the FiveAM suite
#   make run     run the application from source, unbundled
#   make app     build "build/Image Describer.app"
#   make icon    draw res/icon.png again, from tools/icon.lisp
#   make screenshot [INPUT=folder]  draw the window into doc/, describing FOLDER
#                (by default copies of the account pictures macOS ships)
#   make staged  doc/staged.png: the window part way through a review, on a backdrop
#   make notarize  have Apple notarise the bundle (Developer ID, NOTARY_PROFILE)
#   make dist    build/Image Describer.zip, the notarised bundle to hand out
#   make dmg     build/Image Describer.dmg of the bundle as it is (DMG=name to rename)
#   make install-cli  put the image-describe command in $(PREFIX)/bin
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

# A profile made once with `xcrun notarytool store-credentials NAME`.
NOTARY_PROFILE ?= notary
PREFIX ?= /usr/local
APP = build/Image Describer.app

.PHONY: deps test run app icon samples screenshot staged notarize dist dmg install-cli clean

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

# The account pictures in /Library/User Pictures, in a folder whose name
# reads well in the picture.
SAMPLES = $(CURDIR)/build/Sample Pictures
INPUT ?= $(SAMPLES)

samples:
	mkdir -p "$(SAMPLES)/Animals" "$(SAMPLES)/Flowers" "$(SAMPLES)/Nature"
	for d in Animals Flowers Nature; do \
	  cp "/Library/User Pictures/$$d/"*.heic "$(SAMPLES)/$$d/" ; done
	xattr -d com.apple.metadata:kMDItemFinderComment "$(SAMPLES)"/*/*.heic 2>/dev/null || true

screenshot: $(if $(filter $(SAMPLES),$(INPUT)),samples)
	INPUT="$(INPUT)" $(LISP) --eval '(asdf:load-system "image-describe")' \
	        --load tools/screenshot.lisp \
	        --eval '(image-describe-screenshot:main)'

staged:
	$(LISP) --eval '(asdf:load-system "image-describe")' \
	        --eval '(asdf:load-system "image-describe/tests")' \
	        --load tools/staged.lisp \
	        --eval '(image-describe-staged:main)'

notarize:
	@codesign -dv "$(APP)" 2>&1 | grep -q "Authority=Developer ID Application" || \
	  { echo "$(APP) is not signed with a Developer ID: set MACOS_SIGNING_IDENTITY (local.mk) and make app"; exit 1; }
	$(LISP) --eval '(asdf:load-system "asdf-macos-app")' \
	        --eval '(macos-app:notarize "$(APP)" :keychain-profile "$(NOTARY_PROFILE)")'
	spctl --assess --type execute --verbose "$(APP)"

dist: notarize
	cd build && rm -f "Image Describer.zip" && \
	  ditto -c -k --keepParent "Image Describer.app" "Image Describer.zip"
	@echo "build/Image Describer.zip"

# The bundle and a link to /Applications, to drag it onto.  Signed with
# MACOS_SIGNING_IDENTITY when there is one; notarise the bundle first for a
# disk image that opens without a warning.
DMG ?= build/Image Describer.dmg

dmg:
	rm -rf build/dmg "$(DMG)"
	mkdir -p build/dmg
	ditto "$(APP)" "build/dmg/Image Describer.app"
	ln -s /Applications build/dmg/Applications
	hdiutil create -volname "Image Describer" -srcfolder build/dmg -fs HFS+ \
	  -format UDZO -imagekey zlib-level=9 -ov "$(DMG)"
	rm -rf build/dmg
	$(if $(MACOS_SIGNING_IDENTITY),codesign --sign "$(MACOS_SIGNING_IDENTITY)" --timestamp "$(DMG)")
	@echo "$(DMG)"

install-cli:
	install -d "$(PREFIX)/bin"
	install -m 755 tools/image-describe "$(PREFIX)/bin/image-describe"

clean:
	rm -rf build
	find . -name '*.fasl' -not -path './ocicl/*' -delete
