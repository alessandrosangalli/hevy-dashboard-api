const { exec } = require("child_process");
module.exports = (req, res) => {
  exec("./hevy-dashboard-api-exe", (error, stdout) => {
    if (error) {
      res.status(500).send("Erro ao rodar API");
      return;
    }
    res.status(200).send(stdout);
  });
};