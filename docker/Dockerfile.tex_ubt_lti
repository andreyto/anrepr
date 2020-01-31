FROM ubuntu:bionic
ENV RSTUDIO_VER=1.2.5033

RUN ln -snf /usr/share/zoneinfo/Etc/UTC /etc/localtime \
    && echo "Etc/UTC" > /etc/timezone \
    && apt-get update \
    && apt-get upgrade -y \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
       texlive-full texlive-latex-extra texmaker xzdec \
    && rm -rf /var/lib/apt/lists/*

## DT needs to be reinstalled with install2.r - some dependency files are missing from deb
## Same is for rmarkdown - devtools will still ask to install it after deb
## dplyr is outdated and plotly is broken (extra dir level above the js library)
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		littler \
        r-cran-littler \
		r-base \
		r-base-dev \
		r-recommended \
        r-cran-tikzdevice r-cran-knitr pandoc qpdf r-cran-devtools r-cran-testthat r-cran-xml2 \
        r-cran-plotly r-cran-dt r-cran-ggplot2 r-cran-markdown git r-cran-formatr \
	&& ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
	&& install.r docopt \
    && install2.r pander roxygen2 threejs DT rmarkdown dplyr plotly \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*

RUN useradd rstudio \
    && echo "rstudio:rstudio" | chpasswd \
	&& mkdir /home/rstudio \
	&& chown rstudio:rstudio /home/rstudio \
	&& addgroup rstudio staff

## xz-utils is needed for gdebi to work with rstudio-server deb
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
       gdebi-core xz-utils systemd-sysv curl \
    && curl https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.2.5033-amd64.deb > rstudio-server-amd64.deb \
    && gdebi -n rstudio-server-amd64.deb \
    && systemctl enable rstudio-server \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		ed \
		less \
		locales \
		vim-tiny \
		wget \
		ca-certificates \
		fonts-texgyre \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

EXPOSE 8787
CMD service rstudio-server start && sleep infinity

