FROM haskell:7.8
RUN apt-get update && apt-get install -y graphviz
RUN cabal update &&  && cabal install 'graphviz ==2999.*' 'parsec ==3.1.*'
ADD . /src
WORKDIR /src
RUN cabal configure
RUN cabal build
RUN cp /src/dist/build/erd/erd /usr/bin
ENTRYPOINT [ "erd" ]
CMD [ "--help" ]
