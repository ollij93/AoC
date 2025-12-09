package template

type Solution struct{}

func (*Solution) Solve(inp string) (uint, uint) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type puzzle struct {
}

func (puzz *puzzle) solve() (retA, retB uint) {
	return
}

func parse(inp string) puzzle {
	return puzzle{}
}
