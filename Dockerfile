FROM rocker/rstudio:4.5.3

LABEL maintainer="Martin Petr <mp@bodkan.net>"

ARG VERSION

############################################################
# setup the base system
############################################################

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -yqq --no-install-recommends \
        build-essential \
        cmake \
        curl \
        gdal-bin \
        git \
        htop \
        iputils-ping \
        less \
        libblas-dev \
        libbz2-dev \
        libcurl4-openssl-dev \
        libffi-dev \
        libfftw3-dev \
        libfontconfig1-dev \
        libfribidi-dev \
        libgdal-dev \
        libgit2-dev \
        libglpk-dev \
        libglu1 \
        libgsl-dev \
        liblapack-dev \
        libharfbuzz-dev \
        libmagick++-dev \
        libnlopt-cxx-dev \
        libpopt-dev \
        libreadline-dev \
        librsvg2-dev \
        libsnappy1v5 \
        libssl-dev \
        libsqlite3-dev \
        libtiff5-dev \
        libudunits2-dev \
        libunwind-dev \
        libwebp-dev \
        libxml2-dev \
        libxt6 \
        libzmq5 \
        libzstd-dev \
        man-db \
        parallel \
        rename \
        texlive \
        tmux \
        tree \
        unminimize \
        vim \
        wget \
        zlib1g-dev

# fix 'Errors were encountered while processing: fontconfig' during unminimize
RUN fc-cache -f; yes | unminimize

# the container is intended as a dedicated environment to be run in a rootless setting
ENV HOME="/root"

############################################################
# compile and install third-party software dependencies
############################################################

# compile fzf
RUN git clone --depth 1 https://github.com/junegunn/fzf.git ${HOME}/.fzf; ${HOME}/.fzf/install

# compile SLiM
RUN cd /tmp; wget https://github.com/MesserLab/SLiM/archive/refs/tags/v5.1.tar.gz -O slim.tar.gz; \
    tar xf slim.tar.gz; cd SLiM-*; mkdir build; cd build; cmake ..; make slim eidos

# install all compiled software into $PATH
RUN cd /tmp; cp SLiM-*/build/slim SLiM-*/build/eidos /usr/bin

############################################################
# install R packages required by the project
############################################################

# location for the whole project (scripts, notebooks, and data) inside the container
ENV PROJECT=/project
WORKDIR $PROJECT

# install dependencies and setup the slendr Python environment
RUN R -e 'install.packages(c("pak", "devtools"))'
COPY ./ /tmp/slendr
RUN cd /tmp/slendr; \
    if [ "$VERSION" != "dev" ]; then git checkout $VERSION; fi; \
    R -e 'pak::local_install(".", dependencies = TRUE)'

# set the necessary R environment variables for the container
RUN printf "PATH=$PATH\nSLENDR_UV=TRUE\n" > ${HOME}/.Renviron

############################################################
# final configuration steps
############################################################

# clone shell configuration files into the container
RUN cd ${HOME}; git clone https://github.com/bodkan/dotfiles .dotfiles/; rm -f .bashrc .profile; \
    cd .dotfiles; ./install.sh

# make sure the project is ready when RStudio Server session starts
# https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/session_startup_scripts.html
# https://community.rstudio.com/t/how-to-set-the-default-startup-project-in-rocker-tidyverse/63092/2
RUN echo "\nsetHook('rstudio.sessionInit', \(new) if (new && is.null(rstudioapi::getActiveProject())) rstudioapi::openProject('${PROJECT}'))" >> ${HOME}/.Rprofile

# remove compilation sources and other redundant files
RUN rm -r /tmp/* /home/rstudio
