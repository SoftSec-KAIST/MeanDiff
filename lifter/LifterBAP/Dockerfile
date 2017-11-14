FROM build_base
MAINTAINER Soomin Kim <soomink@kaist.ac.kr>

RUN ls
RUN apt install binutils-multiarch -y
RUN apt install clang -y
RUN apt install debianutils -y
RUN apt install libgmp-dev -y
RUN apt install libzip-dev -y
RUN apt install llvm-3.8-dev -y
RUN apt install llvm-dev -y
RUN apt install m4 -y
RUN apt install ncurses-dev -y
RUN apt install perl -y
RUN apt install pkg-config -y
RUN apt install zlib1g-dev -y
RUN apt install time -y
RUN apt install libcurl4-gnutls-dev -y

RUN apt install opam -y
RUN opam init --comp=4.02.3 -y
RUN eval `opam config env`
RUN opam install depext

RUN opam pin add bap --dev-repo

# src deps
RUN opam install core_kernel yojson
