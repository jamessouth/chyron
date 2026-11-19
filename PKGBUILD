# Maintainer: baldric <jamesdanielsouth [at] gmail [dot] com>

pkgname=chyron-bin
pkgver=0.0.3
pkgrel=1
pkgdesc='A CLI text scroller/split-flap for Linux, written in OCaml'
arch=('x86_64')
url='https://github.com/jamessouth/chyron'
license=('MIT')
depends=('glibc')
provides=('chyron')
conflicts=('chyron')
source=(
  "${pkgname}-${pkgver}::${url}/releases/download/v${pkgver}/chyron"
  "${pkgname}-${pkgver}-LICENSE::${url}/releases/download/v${pkgver}/LICENSE"
)
b2sums=('f')
package() {
  install -Dm755 "${srcdir}/${pkgname}-${pkgver}" "${pkgdir}/usr/bin/chyron"
  install -Dm644 "${srcdir}/${pkgname}-${pkgver}-LICENSE" "${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
}


#makepkg -si                  # Builds and installs the package locally to test it
#namcap PKGBUILD *.pkg.tar.*  # Checks for any Arch packaging warnings or missing depends
#makepkg --printsrcinfo > .SRCINFO  # Generates the .SRCINFO file required by the AUR

