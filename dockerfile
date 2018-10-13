FROM openjdk:8

RUN \
  curl -L -o sbt-1.1.6.deb http://dl.bintray.com/sbt/debian/sbt-1.1.6.deb && \
  dpkg -i sbt-1.1.6.deb && \
  rm sbt-1.1.6.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

WORKDIR /hookahbot

ADD . /hookahbot

CMD sbt run