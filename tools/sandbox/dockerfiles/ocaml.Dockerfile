FROM ocaml/opam:debian-12
WORKDIR /app
COPY . /app
CMD ["bash"]
