# Maintainer: baldric <jamesdanielsouth [at] gmail [dot] com>

pkgname=chyron-bin
pkgver=0.0.3
pkgrel=1
pkgdesc='A CLI text scroller/split-flap for Linux, written in OCaml'
arch=('x86_64')
url='https://github.com/jamessouth/chyron'
license=('MIT')
options=('!debug')
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

