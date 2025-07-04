# Copyright 1999-2012 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

EAPI=7
inherit cmake

DESCRIPTION="Multitouch gesture recognizer"
HOMEPAGE="https://github.com/JoseExposito/touchegg"
SRC_URI="https://github.com/JoseExposito/touchegg/archive/${PV}.tar.gz -> ${P}.tar.gz"

LICENSE="GPL-3"
SLOT="0"
KEYWORDS="~amd64"
IUSE="gtk"

RDEPEND="
	dev-libs/libinput
	dev-libs/pugixml
	x11-libs/cairo
	x11-libs/libX11
	x11-libs/libXtst
	x11-libs/libXrandr
	x11-libs/libXi
	dev-libs/glib:2
	gtk? ( x11-libs/gtk+:3 )
	virtual/libudev
"
DEPEND="${RDEPEND}"

src_configure() {
	local mycmakeargs=(
		-DAUTO_COLORS="$(usex gtk)"
	)

	cmake_src_configure
}

src_install(){
	cmake_src_install
        doinitd "${FILESDIR}"/${PN}
}
