import cfgclasses
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Config:
    day: int = cfgclasses.arg("Number of the day to generate a new entry for.")

    def run(self) -> None:
        proj_dir = Path(__file__).parent
        # Get the example data and values from the user
        data_dir = proj_dir / f"data/day{self.day}"
        data_dir.mkdir(exist_ok=True)
        example_file = data_dir / "example.txt"
        if not example_file.exists():
            print("Input example data, ending in a blank line:")
            example_text = ""
            while inp := input():
                example_text += inp + "\n"
            example_file.write_text(example_text)
        p1_answer = input("Input the example answer for part 1:")
        p2_answer = input(
            "Input the example answer for part 2 (or leave blank if not yet available):"
        )

        # Update __main__.py
        main = proj_dir / "__main__.py"
        main_contents = main.read_text()
        new_main_contents = []
        for line in main_contents.splitlines():
            if line.strip() == "# Import new days solutions here...":
                new_main_contents.append(f"    day{self.day},")
            if line.strip() == "# Register new days solutions here...":
                new_main_contents.append(
                    f'    Day("day{self.day}", day{self.day}.p1, {p1_answer or "None"}, '
                    f'day{self.day}.p2, {p2_answer or "None"}),'
                )
            new_main_contents.append(line)
        main.write_text("\n".join(new_main_contents))

        template_file = proj_dir / "day.template"
        new_py_file = proj_dir / f"day{self.day}.py"
        if not new_py_file.exists():
            new_py_file.write_text(template_file.read_text())

        real_file = data_dir / "real.txt"
        if not real_file.exists():
            print("Input the real data, ending in a blank line:")
            real_text = ""
            while inp := input():
                real_text += inp + "\n"
            real_file.write_text(real_text)


if __name__ == "__main__":
    cfg = cfgclasses.parse_args(Config, sys.argv[1:], "newday")
    cfg.run()
