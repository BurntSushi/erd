FROM centos:7
RUN yum -y install wget
RUN yum -y install git
RUN yum -y install 'graphviz*'
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN git clone https://github.com/BurntSushi/erd.git
RUN cd /erd && \
    stack init && \
    stack setup && \
    stack build
