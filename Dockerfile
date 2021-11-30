# https://docs.docker.com/engine/reference/builder/#from
#   "The FROM instruction initializes a new build stage and sets the
#    Base Image for subsequent instructions."
FROM erlang:20.3.8.1-alpine as builder
# https://docs.docker.com/engine/reference/builder/#label
#   "The LABEL instruction adds metadata to an image."
LABEL stage=builder

# Install git for fetching non-hex depenencies. Also allows epm
# to find it's own git version.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk add --update openssh-client git

# WORKDIR is located in the image
#   https://docs.docker.com/engine/reference/builder/#workdir
WORKDIR /root/epm

# copy the entire src over and build
COPY . .
RUN ./bootstrap

# this is the final runner layer, notice how it diverges from the original erlang
# alpine layer, this means this layer won't have any of the other stuff that was
# generated previously (deps, build, etc)
FROM erlang:24-alpine as runner

# copy the generated `epm` binary over here
COPY --from=builder /root/epm/_build/prod/bin/epm .

# and install it
RUN HOME=/opt ./epm local install \
    && rm -f /usr/local/bin/epm \
    && ln /opt/.cache/epm/bin/epm /usr/local/bin/epm \
    && rm -rf epm

# simply print out the version for visibility
ENTRYPOINT ["/usr/local/bin/epm"]
CMD ["--version"]

