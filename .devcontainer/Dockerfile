# change source image if necessary
# see 
FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.2

# key dependencies for utilities
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | gpg --dearmor > /usr/share/keyrings/yarn-archive-keyring.gpg
RUN apt-get update -qq \
  && export DEBIAN_FRONTEND=noninteractive \
  && apt-get -y install --no-install-recommends libxt6 curl libglpk-dev libcurl4-openssl-dev libssl-dev libxml2-dev libarchive-dev fonts-dejavu-core fonts-dejavu-extra

# install rpython and pip3 just in case
RUN apt-get update \
  && export DEBIAN_FRONTEND=noninteractive \
  && apt-get -y install --no-install-recommends python3-setuptools python3-pip

# install R package dependencies
# add / remove packages from line below (but keep httpgd, renv, languageserver
RUN install2.r renv languageserver

