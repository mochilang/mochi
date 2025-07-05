FROM wasmerio/wasmer:latest
WORKDIR /app
COPY . /app
CMD ["bash"]
