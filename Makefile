run:
	pipenv run python main.py

updateProto:
	pipenv run python -m grpc_tools.protoc -Iprotos --python_out=. --grpc_python_out=. protos/faceDataServer.proto
