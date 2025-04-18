FROM haskell:9.2.8
WORKDIR /app
COPY . .
ARG HEVY_API_KEY
ENV HEVY_API_KEY=$HEVY_API_KEY
RUN stack setup --install-ghc
RUN stack build --copy-bins
CMD ["stack", "exec", "hevy-dashboard-api-exe"]
EXPOSE $PORT