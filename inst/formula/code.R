
as.data.frame.ExpressionSet = function(x, ...) as(x, "data.frame")

setAs("ExpressionSet", "data.frame", function (from)
{
    data.frame(t(exprs(from)), pData(from))
})

