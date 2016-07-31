deploy:
	rhc apps -v | grep "SSH.*kirosprime" | awk '{print $$2;}' | xargs -I{} scp -r target/scala-2.11/kiros-prime_* {}:~/app-root/data/.
