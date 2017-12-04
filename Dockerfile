FROM build_base
MAINTAINER Soomin Kim <soomink@kaist.ac.kr>

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/ubuntu xenial main" | tee /etc/apt/sources.list.d/mono-official.list
RUN apt-get update -y

RUN apt install ca-certificates-mono -y
RUN apt install mono-complete -y
RUN apt install fsharp -y
RUN apt install wget -y

# RUN mkdir /compile
# ADD ./src/ /compile/

# WORKDIR /compile
# RUN mkdir /compile/nuget
# RUN wget -O ./nuget/nuget.exe \
#       https://dist.nuget.org/win-x86-commandline/v2.8.6/nuget.exe
# RUN mono ./nuget/nuget.exe install FAKE -OutputDirectory packages \
#       -ExcludeVersion

# RUN make
