FROM haskell:9.2.8
WORKDIR /app
COPY . .
ARG HEVY_API_KEY
ENV HEVY_API_KEY=$HEVY_API_KEY
RUN stack build --copy-bins --local-bin-path /app/bin
CMD ["/app/bin/hevy-dashboard-api-exe"]