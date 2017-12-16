function eval_move!(move, dancers)
    move_type = move[1]
    params = move[2:end]
    if move_type == 's' # spin
        eval_spin!(params, dancers)
    elseif move_type == 'x' # exchange
        eval_exchange!(params, dancers)
    elseif move_type == 'p' # partner swap
        eval_partner!(params, dancers)
    end
end

function eval_spin!(params, dancers)
    shift = parse(Int, params)
    dancers[1:end] = circshift(dancers, shift)
end

function eval_exchange!(params, dancers)
    i, j = map(x -> parse(Int, x) + 1, split(params, "/"))
    dancers[i], dancers[j] = dancers[j], dancers[i]
end

function eval_partner!(params, dancers)
    a, b = split(params, "/")
    ia = findfirst([x == a for x in dancers])
    ib = findfirst([x == b for x in dancers])
    dancers[ia] = b
    dancers[ib] = a
end

function dance!(moves, dancers)
    for m in moves
        eval_move!(m, dancers)
    end
end

moves = split(readchomp(STDIN), ",")
dancers = collect(join(c) for c in 'a':'p')
orig_dancers = copy(dancers)
dance_cycle = [orig_dancers]

dance!(moves, dancers)
println(join(dancers))

while dancers != orig_dancers
    push!(dance_cycle, copy(dancers))
    dance!(moves, dancers)
end

println(join(dance_cycle[1_000_000_000 % length(dance_cycle) + 1]))
