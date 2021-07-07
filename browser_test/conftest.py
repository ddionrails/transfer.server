import pytest


@pytest.mark.usefixtures("tmpdir_factory")
@pytest.fixture(scope="class")
def download_dir(
    request: pytest.FixtureRequest, tmpdir_factory: pytest.TempdirFactory
) -> None:
    setattr(request.cls, "download_dir", tmpdir_factory.mktemp("Download"))
