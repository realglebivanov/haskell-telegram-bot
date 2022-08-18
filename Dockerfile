FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y curl libpq-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup

RUN mkdir /app
WORKDIR /app

ADD ./stack.yaml /app/stack.yaml
ADD ./stack.yaml.lock /app/stack.yaml.lock
ADD ./package.yaml /app/package.yaml
RUN stack build --only-dependencies

ADD ./app /app/app
ADD ./src /app/src
RUN stack build
