using QuickTypes
using MacroTools: @q, @capture

macro mymodel(def)
    @capture(def, model_(; params__))
    esc(@q begin
            $QuickTypes.@qstruct_fp $model(; $(params...))
        end)
end
@mymodel modelA(; p0=0f0)
