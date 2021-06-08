using Erjulix
using Documenter

DocMeta.setdocmeta!(Erjulix, :DocTestSetup, :(using Erjulix); recursive=true)

makedocs(;
    modules=[Erjulix],
    authors="Paul Bayer",
    repo="https://github.com/pbayer/erjulix/blob/{commit}{path}#{line}",
    sitename="erjulix",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://pbayer.github.io/erjulix",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/pbayer/erjulix",
)
