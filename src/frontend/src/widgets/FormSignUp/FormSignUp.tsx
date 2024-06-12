import { LockOutlined, UserOutlined } from '@ant-design/icons'
import { zodResolver } from '@hookform/resolvers/zod'
import { Alert, Button, Form, Input, Popover, Select, Space, Typography } from 'antd'
import { ReactNode, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { getIsPasswordStrong } from '~/entities/Password'
import { UserSignUpDto, userSignUpSchema } from '~/entities/User'
import { UserRole } from '~/entities/UserRole'
import { getIsDetailedApiError } from '~/shared/apiClient'

interface FormSignUpProps {
  children: ReactNode
  onSubmit: SubmitHandler<UserSignUpDto>
  roles: UserRole[]
}

export function FormSignUp({ children, onSubmit, roles }: FormSignUpProps) {
  const { t } = useTranslation(FormSignUp.name)

  const {
    handleSubmit,
    control,
    formState: { errors },
    setError,
  } = useForm<UserSignUpDto>({
    mode: 'onBlur',
    values: {
      login: '',
      password: '',
      role: 0, // TODO empty by default
    },
    resolver: zodResolver(userSignUpSchema, { async: true }),
  })

  const [submissionError, setSubmissionError] = useState('')

  return (
    <Form
      className="w-4/5 m-auto"
      labelCol={{ span: 3, offset: 5 }}
      wrapperCol={{ span: 8 }}
      layout="horizontal"
      onFinish={async (...args) => {
        try {
          await handleSubmit(onSubmit)(...args)
        } catch (error) {
          if (!getIsDetailedApiError(error)) throw error

          switch (error.code) {
            case 'SIGN_UP_LOGIN_TAKEN': {
              setError('login', { message: t('loginTaken') }, { shouldFocus: true })
              return
            }
            default: {
              setSubmissionError(error.code)
            }
          }
        }
      }}
    >
      <Controller
        name="role"
        control={control}
        render={({ field }) => (
          <Form.Item
            required
            label={t('role')}
            help={errors.role?.message}
            validateStatus={errors.role?.message && 'error'}
          >
            <Select
              options={roles.map(({ id, role }) => ({ value: id, label: t(`role${role}`) }))}
              placeholder={t('select')}
              {...field}
            />
          </Form.Item>
        )}
      />
      <Controller
        name="login"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('login')}
            required
            help={errors.login?.message}
            validateStatus={errors.login?.message && 'error'}
          >
            <Popover
              overlayClassName="max-w-prose"
              content={
                <Alert
                  showIcon
                  message={
                    <Typography.Text className="text-xs text-slate-500">
                      {t('loginFieldTip')}
                    </Typography.Text>
                  }
                />
              }
              trigger="focus"
              placement="right"
            >
              <div>
                <Input
                  prefix={<UserOutlined className="text-slate-400" />}
                  placeholder={t('loginFieldPlaceholder')}
                  {...field}
                />
              </div>
            </Popover>
          </Form.Item>
        )}
      />
      <Controller
        name="password"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('password')}
            required
            help={errors.password?.message}
            validateStatus={errors.password?.message && 'error'}
          >
            <Popover
              overlayClassName="max-w-prose"
              content={
                <Alert
                  showIcon
                  message={
                    <Typography.Text className="text-xs text-slate-500">
                      {t('passwordFieldTipAplhabet')},{' '}
                      <code
                        className="font-bold text-slate-700"
                        title="- _ + ! ? = # $ % & @ ^ ` ~"
                      >
                        - _ + ! ? = # $ % & @ ^ ` ~
                      </code>{' '}
                      {t('passwordFieldTipLength')}
                    </Typography.Text>
                  }
                />
              }
              trigger="focus"
              placement="right"
            >
              <div>
                <Input.Password
                  prefix={<LockOutlined className="text-slate-400" />}
                  placeholder={t('passwordFieldPlaceholder')}
                  suffix={
                    <Typography.Text className="text-xs text-slate-400">
                      {getIsPasswordStrong(field.value) ? t('Reliable') : t('Weak')}
                    </Typography.Text>
                  }
                  {...field}
                />
              </div>
            </Popover>
          </Form.Item>
        )}
      />

      <Form.Item
        label=" "
        colon={false}
      >
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
            {t('submit')}
          </Button>

          {!submissionError ? null : (
            <Typography.Text type="danger">{submissionError}</Typography.Text>
          )}

          {children}
        </Space>
      </Form.Item>
    </Form>
  )
}
