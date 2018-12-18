FROM centos:7 as build
ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:~/.local/bin LANG=en_US.UTF-8

RUN yum install wget make gcc perl gmp-devel zlib-devel -y && \
    yum clean all

RUN wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz -O -|tar xpfz - -C /tmp && \
    mkdir -p ~/.local/bin && \
    cp /tmp/stack-*/stack ~/.local/bin && \
    rm -rf /tmp/stack-*

COPY . /root/haskseg

WORKDIR /root/haskseg

RUN stack build

FROM centos:7

ENV LANG=en_US.UTF-8

RUN yum install gmp-devel zlib-devel -y && \
    yum clean all
    
RUN mkdir /opt/haskseg

WORKDIR /opt/haskseg

COPY --from=build /root/haskseg/.stack-work/install/x86_64-linux/lts-12.19/8.4.4/bin/haskseg .

ENTRYPOINT ["/opt/haskseg/haskseg", "--"]
