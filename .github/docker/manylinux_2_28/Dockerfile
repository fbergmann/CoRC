FROM quay.io/pypa/manylinux_2_28_x86_64

WORKDIR /build

RUN python3 -m venv env-build \
    && ./env-build/bin/pip install swig==3.0.12 cmake ninja mkl-static==2025.3.1 \
    && yum update -y \
    && yum install -y openssl-devel R-core-devel wget gcc-toolset-14-gcc-plugin-annobin pandoc libxml2-devel git harfbuzz-devel fribidi-devel fontconfig-devel freetype-devel libpng-devel libtiff-devel libjpeg-devel libwebp-devel \
    && yum clean all

ENV PATH="/build/env-build/bin:$PATH"

RUN R -e 'chooseCRANmirror(ind = 1); install.packages(c("remotes", "sessioninfo", "testthat", "vctrs", "purrr", "stringr", "readr", "isnullptr", "assertthat", "knitr", "rmarkdown", "backports", "devtools", "rlang"))' 
