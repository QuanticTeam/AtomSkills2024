import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Button, Checkbox, Form, Input, Space, Typography } from 'antd'
import { useContext, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { Link } from 'react-router-dom'
import { z } from 'zod'
import { PageUnauthorized } from '~/layouts/PageUnauthorized'
import { AuthContext } from '~/shared/auth'
import { apiClient, getIsDetailedApiError } from '~/shared/apiClient'
import { Logo } from '~/shared/ui'

const loginSchema = z.object({
  login: z.string().trim().min(1, ' '),
  password: z.string().trim().min(1, ' '),
  remember: z.boolean(),
})

type LoginData = z.infer<typeof loginSchema>

export default function PageLogin() {
  const { login } = useContext(AuthContext)
  const [submissionError, setSubmissionError] = useState('')

  const {
    handleSubmit,
    control,
    formState: { errors },
  } = useForm<LoginData>({
    values: {
      login: '',
      password: '',
      remember: true,
    },
    resolver: zodResolver(loginSchema),
  })

  const onSubmit: SubmitHandler<LoginData> = async data => {
    try {
      login((await apiClient.post('/User/Login', data)).data)
    } catch (error: unknown) {
      if (getIsDetailedApiError(error)) {
        if (error.code === 'LOGIN_WRONG_CREDENTIALS') {
          setSubmissionError('Неправильный логин или пароль')
          return
        }

        setSubmissionError(error.code)
        return
      }

      throw error
    }
  }

  return (
    <PageUnauthorized>
      <div className="w-72 flex mx-auto mb-16">
        <Logo full />
      </div>

      <div className="flex">
        <Form
          onFinish={handleSubmit(onSubmit)}
          className="w-96 mx-auto"
        >
          <Controller
            name="login"
            control={control}
            render={({ field }) => (
              <Form.Item
                help={errors.login?.message}
                validateStatus={errors.login?.message && 'error'}
              >
                <Input
                  prefix={<UserOutlined className="text-gray-400" />}
                  placeholder="Пользователь"
                  {...field}
                />
              </Form.Item>
            )}
          />
          <Controller
            name="password"
            control={control}
            render={({ field }) => (
              <Form.Item
                help={errors.password?.message}
                validateStatus={errors.password?.message && 'error'}
              >
                <Input.Password
                  prefix={<LockOutlined className="text-gray-400" />}
                  type="password"
                  placeholder="Пароль"
                  {...field}
                />
              </Form.Item>
            )}
          />

          <Controller
            name="remember"
            control={control}
            render={({ field }) => (
              <Form.Item>
                <div className="flex justify-between items-center">
                  <Form.Item
                    name="remember"
                    noStyle
                    valuePropName="checked"
                  >
                    <Checkbox {...field}>Запомнить меня</Checkbox>
                  </Form.Item>

                  <Typography.Text>
                    <Link to="/forgot">Не помню пароль</Link>
                  </Typography.Text>
                </div>
              </Form.Item>
            )}
          />

          <Form.Item>
            <Space
              className="w-full"
              direction="vertical"
              classNames={{ item: 'flex justify-center' }}
            >
              <Button
                type="primary"
                htmlType="submit"
                className="w-full"
              >
                Войти
              </Button>
              {!submissionError ? null : (
                <Typography.Text type="danger">{submissionError}</Typography.Text>
              )}
              <Typography.Text>
                или <Link to="/register">зарегистрироваться</Link>
              </Typography.Text>
            </Space>
          </Form.Item>
        </Form>
      </div>
    </PageUnauthorized>
  )
}
