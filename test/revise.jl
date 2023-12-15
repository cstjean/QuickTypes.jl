using Revise
using Test

## utils

function cp_content(fname; info="test/revise.jl `cp_content`")
    content_path = joinpath(assets_dir, fname)
    test_content_path = joinpath(test_src_dir, content_fname)
    @info info content_path test_content_path
    cp(content_path, test_content_path; force=true)
    chmod(test_content_path, 0o664)  # make sure the file we copied has 'rw' access
end

function do_revise()
    sleep(1)
    Revise.revise()
    sleep(1)
end

## end utils

## setup

assets_dir = joinpath(@__DIR__, "assets")
test_dir = mktempdir()
content_fname = "content.jl"

# use `rand` to avoid caching issues
# when invoking `include("test/revise.jl")` in the REPL
mod_name = string("QuickTypesReviseTestModule", rand(1:100000))
test_src_dir = mkpath(joinpath(test_dir, mod_name, "src"))
test_mod_path = joinpath(test_src_dir, mod_name * ".jl")

open(joinpath(test_mod_path), "w") do f
    write(f, """
module $mod_name
include("$content_fname")
end""")
end
cp_content("blank.jl")

# load that temporary package
mod_sym = Symbol(mod_name)
push!(LOAD_PATH, test_dir)
@eval import $mod_sym
pop!(LOAD_PATH)
TestModule = @eval $mod_sym

## end setup

@testset "should revise basic content" begin
    cp_content("basic1.jl")
    do_revise()

    @test TestModule.Car(10; manufacturer=("Danone", "Hershey")).nwheels == 4
    @test TestModule.Plane{Int, Symbol}(2).brand == :zoomba

    cp_content("basic2.jl")
    do_revise()

    @test TestModule.Car(10; manufacturer=("Danone", "Hershey")).nwheels == 18
    @test TestModule.Plane{Int, Symbol}(2).brand == :airbus
end

@testset "should revise content within macros" begin
    cp_content("within-macro1.jl")
    do_revise()

    @test TestModule.modelA().p0 == 0f0

    cp_content("within-macro2.jl")
    do_revise()

    @test TestModule.modelA().p0 == 22f0
end
