from shutil import rmtree
from typing import Generator

import pytest


@pytest.mark.usefixtures("tmpdir_factory")
@pytest.fixture(scope="class")
def download_dir(
    request: pytest.FixtureRequest, tmpdir_factory: pytest.TempdirFactory
) -> Generator[None, None, None]:
    directory = tmpdir_factory.mktemp("Download")
    setattr(request.cls, "download_dir", directory)
    yield
    rmtree(str(directory))
