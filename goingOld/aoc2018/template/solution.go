package template

type Solution struct{}

func (*Solution) Solve(inp string) (string, string) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type puzzle struct {
}

func (puzz *puzzle) solve() (retA, retB string) {
	return
}

func parse(inp string) puzzle {
	return puzzle{}
}
