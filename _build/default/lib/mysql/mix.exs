defmodule Mysql.Mixfile do
  use Mix.Project

  def project() do
    [app: :mysql,
     version: "1.8.0",
     elixir: "~> 1.0",
     description: description(),
     package: package(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     aliases: aliases()]
  end

  defp description() do
     """
     MySQL/OTP - Erlang MySQL client driver
     """
  end

  defp package() do
    [contributors: ["Viktor Söderqvist", "Jan Uhlig", "et.al."],
     maintainers: ["Viktor Söderqvist", "TJ"],
     licenses: ["LGPL-3.0-or-later"],
     links: %{"GitHub" => "https://github.com/mysql-otp/mysql-otp"},
     build_tools: ["make", "rebar3", "mix"],
     files: ~w(mix.exs README.md CHANGELOG.md) ++
            ~w(doc erlang.mk include Makefile priv src test)
    ]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application() do
    [applications: [:ssl]]
  end

  # Dependencies
  defp deps() do
    []
  end

  # Alias docs to nothing, just to be able to publish docs to Hex
  # using already generated docs
  defp aliases() do
    [
      docs: []
    ]
  end
end
