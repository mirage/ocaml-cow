OPAM_DEPENDS="re ulex uri xmlm ezjsonm dyntype ounit omd"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.00.1,1.2.*) ppa=avsm/ocaml40+opam12 ;;
4.01.0,1.2.*) ppa=avsm/ocaml41+opam12 ;;
4.02.1,1.2.*) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers opam libssl-dev

if [ "$OCAML_VERSION" != "4.02.0" ]; then
  sudo apt-get install -qq camlp4-extra
fi

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam install ${OPAM_DEPENDS}

eval `opam config env`
opam pin add cow .
opam update
make
cd tests && make

opam install lwt ssl ezjsonm cowabloga
