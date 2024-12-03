import pytest
from . import __main__ as main

@pytest.mark.parametrize(
    "day",
    main.ALL_DAYS,
    ids=[d.name for d in main.ALL_DAYS],
)
def test_day(day: main.Day) -> None:
    """Test the day gets the right data for its example input."""
    data_file = day.data_dir / "example.txt"
    data = data_file.read_text()
    try:
        p1 = day.p1(data)
        assert p1 == day.p1_answer, f"{p1} != {day.p1_answer}"
    except NotImplementedError:
        pass

    try:
        p2 = day.p2(data)
        assert p2 == day.p2_answer, f"{p2} != {day.p2_answer}"
    except NotImplementedError:
        pass
