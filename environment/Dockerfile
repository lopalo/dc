FROM ubuntu:14.04
MAINTAINER Vlad Lopalo

ENV LANG C.UTF-8
ENV PROJECT_ROOT_DIR .
ENV GHC_VERSION 7.8.4
ENV CABAL_VERSION 1.20

COPY ${PROJECT_ROOT_DIR} /dc
EXPOSE 10501

RUN apt-get install software-properties-common -y
RUN sudo add-apt-repository ppa:hvr/ghc -y
RUN apt-get update

RUN apt-get install zlib1g-dev -y

RUN apt-get install python -y
RUN apt-get install python-pip -y
WORKDIR /dc/environment
RUN pip install -r py_requirements.txt

RUN apt-get install ghc-${GHC_VERSION} -y
RUN apt-get install cabal-install-${CABAL_VERSION} -y
ENV PATH ${PATH}:/opt/ghc/$GHC_VERSION/bin:/opt/cabal/$CABAL_VERSION/bin
WORKDIR /dc/server
RUN cabal update
RUN cabal install --only-dependencies

WORKDIR /dc



