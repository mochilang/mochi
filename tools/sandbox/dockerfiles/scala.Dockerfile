FROM hseeberger/scala-sbt:latest
WORKDIR /app
COPY . /app
CMD ["bash"]
