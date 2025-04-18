FROM haskell:9.8.4
WORKDIR /app
COPY . .
ARG HEVY_API_KEY
ENV HEVY_API_KEY=$HEVY_API_KEY
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin
CMD ["/app/bin/hevy-dashboard-api-exe"]