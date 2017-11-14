FROM build_base
MAINTAINER Soomin Kim <soomink@kaist.ac.kr>

RUN apt install m4 debianutils -y

RUN apt install ocaml opam -y
RUN opam init --comp=4.02.3 -y
RUN eval `opam config env`

RUN opam depext --install core yojson zarith zmq -y