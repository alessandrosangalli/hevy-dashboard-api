const http = require("http");
const { exec } = require("child_process");

const handler = (req, res) => {
  exec("./hevy-dashboard-api-exe", (error, stdout) => {
    if (error) {
      res.writeHead(500);
      res.end("Erro ao rodar API");
      return;
    }
    res.writeHead(200);
    res.end(stdout);
  });
};

const server = http.createServer(handler);
server.listen(3000, () => console.log("Rodando na porta 3000"));