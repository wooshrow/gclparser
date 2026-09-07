#
# A Dockerfile that will install GHC 9.5.2, Z3 4.8.12, and Haskell z3-binding.
# This combination works.
# The dockerfile additionally also install Spin 6.5.1.
#
# * How to build the docker image
#      docker build -t <you-can-use-a-tag-id-if-youwant> .
# * How to run the image (so you get a running container) and enter a shell-mode:
#      docker run --rm -it <tag or image id>
# * How to test if z3 and spin work: see comments below
# * How to close the running container: just type exit at its shell.
#   Note that code/data you might produce during your shell interactions with the
#   container are lost unless you save it to e.g. a folder shared with your own
#   local host.
#
#

FROM gcc:latest
RUN apt-get update && \
    apt-get install alpine-pico
# change-dir to ~/ , which is /root
WORKDIR /root 

# installing ghcup, see https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh" && \
    bash -c "curl -sSL https://get.haskellstack.org/ | sh"
# Add ghcup to PATH
ENV PATH=${PATH}:/root/.ghcup/bin
# stack put binaries it builts here, so we also add the path:
ENV PATH=${PATH}:/root/.local/bin
# install ghc 9.2.5
RUN ghcup install ghc 9.2.5 && \
    ghcup set ghc 9.2.5

# install z3 the version you want; here we will use version 4.8.12
# we will install from pre-compiled binary provided in z3 github
# this will install z3 to /root/z3/z3
RUN mkdir z3
WORKDIR /root/z3
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.12/z3-4.8.12-x64-glibc-2.31.zip && \
    unzip *.zip && \
    rm *.zip && \
    mv z3-4.8.12-x64-glibc-2.31 z3

#install Haskell z3-binding:
RUN cabal install --lib z3 --extra-lib-dirs=/root/z3/z3/bin/  --extra-include-dirs=/root/z3/z3/include/

# i'll just close the gcl-parser project so you can immidiately play with it
# After done playing, you may want to delete it and work on your own project
WORKDIR /root
RUN git clone https://github.com/wooshrow/gclparser.git

# This should do it. To test if your haskell-z3 is working try this from the shell 
# inside your container:
#    docker run --rm -it <image-id>
#    runhaskell gclparser/examples/examplesHaskellZ3/TestZ3_simple.hs
#    exit
# 
# The test-file you ran should not crash.

#installing spin.
# Building from src seems to be problematic, so below I'll just use the pre-compiled
# binary for version 6.5.1 (so, one sub-version lower).
#
RUN wget https://github.com/nimble-code/Spin/archive/refs/tags/version-6.5.2.zip && \
    unzip *.zip && \
    rm *.zip && \
    mv Spin-version-6.5.2 spin && \
    gunzip spin\Bin\spin651_linux64.gz 
  
# Testing spin-install
# Test-1, which only check the simulator:
#    /spin/Bin/spin651_linux64 ./spin/Examples/hello.pml
#
# Test-2, test that the verifier works:
#    ./spin/Bin/spin651_linux64 -a ./spin/Examples/loops.pml 
#    cc -DNOREDUCE -o pan pan.c
#    ./pan
