FROM build_base
MAINTAINER Soomin Kim <soomink@kaist.ac.kr>

RUN apt install python -y

RUN mkdir build

WORKDIR build

RUN git clone https://github.com/intelxed/xed.git xed
RUN git clone https://github.com/intelxed/mbuild.git mbuild

RUN xed/mfile.py install

RUN cp ./kits/xed-install-base-`date +%Y-%m-%d`-lin-x86-64/lib/libxed* /usr/lib
RUN chmod 0777 /usr/lib/libxed*

RUN cp -r ./kits/xed-install-base-`date +%Y-%m-%d`-lin-x86-64/include/xed /usr/include/xed
RUN chmod -R 0655 /usr/include/xed
